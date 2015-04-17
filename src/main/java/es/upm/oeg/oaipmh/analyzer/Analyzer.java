package es.upm.oeg.oaipmh.analyzer;

import es.upm.oeg.oaipmh.analyzer.parser.XmlParser;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.io.File;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

@Component
public class Analyzer {

    private static final Logger LOG = LoggerFactory.getLogger(Analyzer.class);

    @Value("${providers.file}")
    File providers;

    @Value("${thread.pool.size}")
    Integer threadPoolSize;

    @Value("${thread.pool.time}")
    Long maxSeconds;

    @Value("${output.directory}")
    File directory;

    @Autowired
    XmlParser xmlParser;

    ConcurrentHashMap<String, String> status;


    public void analyze() throws InterruptedException {

        status = new ConcurrentHashMap<>();

        // Read server from xml file
        List<String> servers = xmlParser.read(providers, "baseURL");

        // Initialize a fixed thread pool
        ExecutorService pool = Executors.newFixedThreadPool(threadPoolSize);

        // Create a new visitor for each server and add it to pool
        for(String server: servers){
            Visitor visitor = new Visitor(directory,status,server);
            pool.execute(visitor);

        }

        // Wait for a maximum time
        pool.awaitTermination(maxSeconds, TimeUnit.SECONDS);

        if (!status.isEmpty()){
            LOG.info("Some servers error: ");

            ConcurrentHashMap.KeySetView<String, String> list = status.keySet();
            for(String server: list){
                LOG.info(server);
            }
        }


        System.exit(0);
    }

}
