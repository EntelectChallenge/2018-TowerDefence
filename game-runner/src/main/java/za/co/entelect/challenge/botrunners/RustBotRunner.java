package za.co.entelect.challenge.botrunners;

import za.co.entelect.challenge.entities.BotMetaData;

import java.io.IOException;

public class RustBotRunner extends BotRunner {
    public RustBotRunner(BotMetaData botMetaData, int timoutInMilis) {
        super(botMetaData, timoutInMilis);
    }

    @Override
    protected String runBot() throws IOException {
        String line = "cargo run --release";
        return RunSimpleCommandLineCommand(line, 0);
    }
}
