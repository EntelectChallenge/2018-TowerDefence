package za.co.entelect.challenge.botrunners;

import za.co.entelect.challenge.entities.BotMetaData;

import java.io.IOException;

public class RustBotRunner extends BotRunner {

    public RustBotRunner(BotMetaData botMetaData, int timeoutInMilliseconds) {
        super(botMetaData, timeoutInMilliseconds);
    }

    @Override
    protected String runBot() throws IOException {
        String line = "\"./" + this.getBotFileName() + "\"";
        return RunSimpleCommandLineCommand(line, 0);
    }

}
