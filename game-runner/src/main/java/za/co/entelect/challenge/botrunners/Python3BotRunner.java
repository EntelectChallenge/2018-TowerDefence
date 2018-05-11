package za.co.entelect.challenge.botrunners;

import za.co.entelect.challenge.entities.BotMetaData;

import java.io.IOException;

public class Python3BotRunner extends BotRunner {

    public Python3BotRunner(BotMetaData botMetaData, int timeoutInMilliseconds) {
        super(botMetaData, timeoutInMilliseconds);
    }

    @Override
    protected String runBot() throws IOException {
        String line;

        if(System.getProperty("os.name").contains("Windows")) {
            line = "py -3 \"" + this.getBotFileName() + "\"";
        } else {
            line = "python3 \"" + this.getBotFileName() + "\"";
        }

        return RunSimpleCommandLineCommand(line, 0);
    }

}