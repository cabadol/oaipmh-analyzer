package es.upm.oeg.oaipmh.analyzer;


import es.upm.oeg.camel.oaipmh.component.OAIPMHHttpClient;
import es.upm.oeg.camel.oaipmh.dataformat.OAIPMHConverter;
import es.upm.oeg.camel.oaipmh.model.*;
import es.upm.oeg.oaipmh.analyzer.writer.CsvWriter;
import org.apache.http.client.ClientProtocolException;
import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;
import org.joda.time.format.ISODateTimeFormat;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.net.ssl.SSLException;
import javax.net.ssl.SSLHandshakeException;
import javax.net.ssl.SSLProtocolException;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.UnmarshalException;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.UnknownHostException;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

public class Visitor implements Runnable {

    private static final Logger LOG = LoggerFactory.getLogger(Visitor.class);
    private final Summary summary;
    private final ConcurrentHashMap<String, String> status;

    private String server;

    private final File directory;
    private final OAIPMHHttpClient httpClient;
    private CsvWriter writer;
    private URI uri;

    public Visitor(File directory, ConcurrentHashMap<String, String> status, String server){
        // Status of the server
        this.status = status;
        // URI of server
        this.server = server;
        // Directory where save the report
        this.directory = directory;
        // Http Client
        this.httpClient = new OAIPMHHttpClient();
        // Initialize Summary
        this.summary = new Summary();

        LOG.info("Visitor for: {} created", server);
    }

    @Override
    public void run() {
        try {
            this.uri = URI.create(server);
            String name = uri.getHost().replace(".", "_");

            // Prepare text writer
            writer = new CsvWriter(CsvWriter.Mode.MINIMAL, directory, name);

            // Request list of records
            listRecords(null);

            // Close writer
            writer.close();

        } catch (SSLProtocolException e){
            status.put(server,"SSL Protocol Exception: Handshake Alert");
        } catch (SSLHandshakeException e){
            status.put(server,"SSL Handshake Exception: Invalid certification path");
        } catch (SSLException e){
            status.put(server,"SSL Exception: Invalid certificate");
        } catch (URISyntaxException e){
            status.put(server,"Invalid URI");
        } catch (UnmarshalException e){
            status.put(server,"Unmarshalled");
        } catch (ClientProtocolException | UnknownHostException e){
            status.put(server,"Server is down");
        } catch (Exception e){
            LOG.error("Fail on process", e);
        }
    }


    private void listRecords(ResumptionTokenType token) throws IOException, URISyntaxException, JAXBException {

        // request 'verb' to remote data provider
        String responseXML = httpClient.doRequest(uri,"ListRecords",null,null,"oai_dc",token);

        // build a java object from xml
        OAIPMHtype responseObject = OAIPMHConverter.xmlToOaipmh(responseXML);

        // Check if error
        //TODO Retries Policy
        if (isAnError(responseObject)){
            return;
        }

        // Analyze records
        process(responseObject);

        // Check if incomplete list
        ResumptionTokenType newToken = responseObject.getListRecords().getResumptionToken();
        if ((newToken != null) && (newToken.getValue() != null) && !(newToken.getValue().startsWith(" "))){
            listRecords(newToken);
        }
    }

    private boolean isAnError(OAIPMHtype message){
        List<OAIPMHerrorType> errors = message.getError();
        if ((errors != null) && (!errors.isEmpty())){
            for (OAIPMHerrorType error: errors){

                switch(error.getCode()){
                    case NO_RECORDS_MATCH:
                    case NO_METADATA_FORMATS:
                    case NO_SET_HIERARCHY:
                        LOG.info("{} / {}",error.getCode(),error.getValue());
                        break;
                    default:
                        LOG.error("Error on [{}] getting records: {}-{}", server,error.getCode(), error.getValue());
                }
            }
            return true;
        }
        return false;
    }

    private void process(OAIPMHtype response) throws IOException {

        // Global counter
        List<RecordType> records = response.getListRecords().getRecord();
        summary.addTotal(records.size());

        for (RecordType record: records){

            // Analyze record
            Map<String,Integer> counters =processRecord(record);

            // Write to csv
            if (!counters.isEmpty()){
                writer.addAll(counters);
                writer.write();
            }
        }



    }


    private Map<String,Integer> processRecord(RecordType record) {

        Map<String,Integer> counters = new HashMap<>();

        // Read metadata information
        MetadataType metadata = record.getMetadata();
        if (metadata == null || metadata.getDc() == null) {
            // Header status=deleted
            summary.addTotalDeleted(1);
            return counters;
        }


        // Read Dublin Core information
        List<JAXBElement<ElementType>> dc = metadata.getDc().getTitleOrCreatorOrSubject();
        if (dc == null) {
            // Invalid publication
            return counters;
        }
        summary.addTotalPublications(1);

        // Read Dublin Core elements
        Iterator<JAXBElement<ElementType>> dcElements = dc.iterator();


        while (dcElements.hasNext()) {

            JAXBElement<ElementType> dcElement = dcElements.next();

            String name     = dcElement.getName().getLocalPart();
            String value    = dcElement.getValue().getValue();
            Format type     = typeOf(name,value);

            String key      = name+"AS"+type.getId();

            Integer counter = counters.get(key);
            if (counter == null){
                counter = 0;
            }
            counter += 1;

            counters.put(key, counter);
        }
        return counters;
    }


    private Format typeOf(String name, String value) {


        if (value == null) {
            return Format.TEXT;
        }

        // Date Format
        if (name.contains("date")) {
            try {

                DateTimeZone timezone = DateTimeZone.forID("Zulu");//UTC

                DateTime date;
                try {
                    date = ISODateTimeFormat.dateTimeNoMillis().withZone(timezone).parseDateTime(value);
                } catch (Exception e) {
                    try {
                        date = ISODateTimeFormat.dateOptionalTimeParser().parseDateTime(value);
                    } catch (Exception e1) {
                        try {
                            date = ISODateTimeFormat.basicDate().parseDateTime(value);
                        } catch (Exception e2) {
                            date = ISODateTimeFormat.basicDateTime().parseDateTime(value);
                        }
                    }
                }

                writer.add(CsvWriter.PUBLISHED, date.toString());
                return Format.DATE;

            } catch (Exception e) {
                LOG.trace("Not a valid DATE: {}", value);
            }
        }

        // URI Format
        try {
            URI path = URI.create(value).resolve(value);
            if ((path.getHost() != null) && (!path.getHost().trim().equals(""))) {
                return Format.URI;
            }
        } catch (Exception e) {
            LOG.trace("Not a valid URI: {}", value);
        }

        // Text Format
        return Format.TEXT;
    }

}
