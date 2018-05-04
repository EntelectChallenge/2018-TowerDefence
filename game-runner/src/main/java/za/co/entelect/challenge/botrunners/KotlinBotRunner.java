package za.co.entelect.challenge.botrunners;

import za.co.entelect.challenge.entities.BotMetaData;

import java.io.IOException;

public class KotlinBotRunner extends BotRunner {

    public KotlinBotRunner(BotMetaData botMetaData, int timeoutInMilliseconds) {
        super(botMetaData, timeoutInMilliseconds);
    }

    @Override
    protected String runBot() throws IOException {
        String line = "java -jar \"" + this.getBotFileName() + "\"";
        return RunSimpleCommandLineCommand(line, 0);
    }

}
