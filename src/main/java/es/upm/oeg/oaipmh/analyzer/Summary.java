package es.upm.oeg.oaipmh.analyzer;

import lombok.Data;

@Data
public class Summary {

    private Long total= 0L;
    private Long totalDeleted= 0L;
    private Long totalPublications = 0L;

    public void addTotal(int increment){

        total += increment;
    }

    public void addTotalDeleted(int increment){
        totalDeleted += increment;
    }

    public void addTotalPublications(int increment){
        totalPublications += increment;
    }

}
