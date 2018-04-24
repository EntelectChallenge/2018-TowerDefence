package za.co.entelect.challenge.entities;

import za.co.entelect.challenge.enums.BotLanguage;

public class BotMetaData {
    private BotLanguage botLanguage;
    private String botLocation;
    private String botFileName;

    public BotMetaData(BotLanguage language, String botLocation, String botFileName){
        this.botLanguage = language;
        this.botLocation = botLocation;
        this.botFileName = botFileName;
    }

    public String getBotLocation(){
        return this.botLocation;
    }

    public String getBotFileName() {
        return this.botFileName;
    }

    public BotLanguage getBotLanguage(){
        return this.botLanguage;
    }
}
