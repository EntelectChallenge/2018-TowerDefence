package za.co.entelect.challenge.botrunners;

import za.co.entelect.challenge.entities.BotMetaData;

import java.io.IOException;

public class CSharpCoreBotRunner extends BotRunner {

    public CSharpCoreBotRunner(BotMetaData botMetaData, int timoutInMilis) {
        super(botMetaData, timoutInMilis);
    }

    @Override
    protected String runBot() throws IOException {
        String line = "dotnet " + this.getBotFileName();
        return RunSimpleCommandLineCommand(line, 0);
    }
}
