package es.upm.oeg.oaipmh.analyzer;


public enum Format {
    TEXT("text"),URI("uri"),DATE("date");

    private final String id;

    Format(String id){
        this.id = id;
    }

    public String getId(){
        return id;
    }
}
