package za.co.entelect.challenge.botrunners;

import za.co.entelect.challenge.entities.BotMetaData;

import java.io.IOException;

public class JavaScriptBotRunner extends BotRunner {
    public JavaScriptBotRunner(BotMetaData botMetaData, int timoutInMilis) {
        super(botMetaData, timoutInMilis);
    }

    @Override
    protected int runBot() throws IOException {
        String line = "node \"" + this.getBotFileName() + "\"";
        return RunSimpleCommandLineCommand(line, 0);
    }
}
