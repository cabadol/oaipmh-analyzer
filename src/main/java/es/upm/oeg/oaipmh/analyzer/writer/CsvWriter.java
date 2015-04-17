package es.upm.oeg.oaipmh.analyzer.writer;


import com.google.common.base.Splitter;
import es.upm.oeg.oaipmh.analyzer.Format;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;

public class CsvWriter {

    private boolean header;

    public enum Mode{
        EXTENDED, MINIMAL;
    }

    public static final String PUBLISHED = "published";

    private static final String LINE_SEPARATOR = System.getProperty("line.separator");

    private static final String SEPARATOR = ",";

    private static final String INITIAL_VALUE = "0";

    private final FileWriter writer;

    private final Iterable<String> terms;

    private final Iterable<String> elements;

    private Map<String, Object> dataline;

    public CsvWriter(Mode mode, File directory, String name) throws IOException {

        this.header = true;

        // Create directory
        directory.mkdirs();

        // Create an empty file
        File csv = new File(directory,name+".csv");
        csv.createNewFile();
        csv.setWritable(true);

        // Initialize file writer
        this.writer = new FileWriter(csv);


        // Get dc:terms
        this.terms = Splitter.on(',').trimResults().omitEmptyStrings().
                split("abstract , accessRights , accrualMethod , accrualPeriodicity , accrualPolicy , " +
                        "alternative , audience , available , bibliographicCitation , conformsTo , contributor , " +
                        "coverage , created , creator , date , dateAccepted , dateCopyrighted , dateSubmitted , " +
                        "description , educationLevel , extent , format , hasFormat , hasPart , hasVersion , identifier , " +
                        "instructionalMethod , isFormatOf , isPartOf , isReferencedBy , isReplacedBy , isRequiredBy , issued , " +
                        "isVersionOf , language , license , mediator , medium , modified , provenance , publisher , references , " +
                        "relation , replaces , requires , rights , rightsHolder , source , spatial , subject , tableOfContents , " +
                        "temporal , title , type , valid");

        // Get dc:elements
        this.elements = Splitter.on(',')
                .trimResults()
                .omitEmptyStrings()
                .split("contributor , coverage , creator , date , description , format , identifier , language , publisher , " +
                        "relation , rights , source , subject , title , type");

        // Initialize dataline
        initialize(mode);

    }

    private void initialize(Mode mode){

        Consumer<? super String> addToDataline = new Consumer<String>() {
            @Override
            public void accept(String s) {

                for(Format format: Format.values()){
                    dataline.put(s+"AS"+format.getId(), INITIAL_VALUE);
                }

            }
        };

        dataline = new HashMap<>();

        switch(mode){
            case EXTENDED: terms.forEach(addToDataline);
            case MINIMAL: elements.forEach(addToDataline);
        }

        // Add published date
        dataline.put(PUBLISHED,INITIAL_VALUE);
    }

    private void reset(){
        Set<String> keys = dataline.keySet();
        for (String key: keys){
            dataline.put(key,INITIAL_VALUE);
        }
    }

    public void add(String key, Object value){
        dataline.put(key,value);
    }

    public void addAll(Map<String,?> values){

        Set<String> keys = values.keySet();
        for(String key: keys){
            dataline.put(key,values.get(key));
        }

    }


    public void write() throws IOException {

        if (this.header){
            header = false;
            StringBuilder line = new StringBuilder();
            Set<String> headers = dataline.keySet();
            for (String label: headers){
                line.append(label).append(SEPARATOR);
            }
            line.deleteCharAt(line.lastIndexOf(SEPARATOR));
            // write to file
            writer.write(line.toString());
            writer.write(LINE_SEPARATOR);
        }

        // Prepare a string buffer
        StringBuilder line = new StringBuilder();
        Set<String> keys = dataline.keySet();
        for (String key: keys){
            line.append(dataline.get(key)).append(SEPARATOR);
            dataline.put(key,INITIAL_VALUE);
        }
        line.deleteCharAt(line.lastIndexOf(SEPARATOR));

        // write to file
        writer.write(line.toString());
        writer.write(LINE_SEPARATOR);

    }

    public void close() throws IOException {
        writer.close();
    }

}
