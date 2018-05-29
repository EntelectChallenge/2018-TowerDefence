package za.co.entelect.challenge.botrunners;

import za.co.entelect.challenge.entities.BotMetaData;
import java.io.IOException;

public class GolangBotRunner extends BotRunner {

    public GolangBotRunner(BotMetaData botMetaData, int timoutInMilis) {
        super(botMetaData, timoutInMilis);
    }

    @Override
    protected String runBot() throws IOException {
        String line = "go run \"" + this.getBotDirectory() + "/" + this.getBotFileName() + "\"";
        return RunSimpleCommandLineCommand(line, 0);
    }

    @Override
    public int getDockerPort() {
        return 9011;
    }
}
