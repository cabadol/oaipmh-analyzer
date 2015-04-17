package es.upm.oeg.oaipmh.analyzer;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ConfigurableApplicationContext;

@SpringBootApplication
public class Application {

    static{
        System.setProperty("javax.xml.accessExternalDTD","all");
    }

        public static void main(String[] args) throws InterruptedException {

            ConfigurableApplicationContext context = SpringApplication.run(Application.class, args);

            Analyzer analyzer = context.getBean(Analyzer.class);
            analyzer.analyze();

        }

}
