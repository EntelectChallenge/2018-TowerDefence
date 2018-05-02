package za.co.entelect.challenge.botrunners;

import za.co.entelect.challenge.entities.BotMetaData;

import java.io.IOException;

public class JavaBotRunner extends BotRunner {
    public JavaBotRunner(BotMetaData botMetaData, int timoutInMilis) {
        super(botMetaData, timoutInMilis);
    }

    @Override
    protected String runBot() throws IOException {
        String line = "java -jar \"" + this.getBotFileName() + "\"";
        return RunSimpleCommandLineCommand(line, 0);
    }
}
