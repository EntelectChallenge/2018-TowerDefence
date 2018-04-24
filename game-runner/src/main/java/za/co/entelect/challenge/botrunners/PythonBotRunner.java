package za.co.entelect.challenge.botrunners;
import za.co.entelect.challenge.entities.BotMetaData;

import java.io.IOException;

public class PythonBotRunner extends BotRunner {
    public PythonBotRunner(BotMetaData botMetaData, int timoutInMilis) {
        super(botMetaData, timoutInMilis);
    }

    @Override
    protected int runBot() throws IOException
    {
        String line = "python \"" + this.getBotDirectory() +"\\"+ this.getBotFileName() + "\"";
        return RunSimpleCommandLineCommand(line, 0);
    }
}
