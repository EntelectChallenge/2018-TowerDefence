package za.co.entelect.challenge.botrunners;

import za.co.entelect.challenge.entities.BotMetaData;

import java.io.IOException;

public class PHPBotRunner extends BotRunner {
  
    public PHPBotRunner(BotMetaData botMetaData, int timeoutInMilliseconds) {
        super(botMetaData, timeoutInMilliseconds);
    }

    @Override
    protected String runBot() throws IOException {
        String line = "php \"" + this.getBotFileName() + "\"";
        return RunSimpleCommandLineCommand(line, 0);
    }

    @Override
    public int getDockerPort() {
        return 9007;
    }
}
