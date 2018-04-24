package za.co.entelect.challenge.botrunners;

import za.co.entelect.challenge.engine.exceptions.InvalidRunnerState;
import za.co.entelect.challenge.entities.BotMetaData;

public class BotRunnerFactory {
    public static BotRunner createBotRunner(BotMetaData botMetaData, int timoutInMilis) throws InvalidRunnerState {
        switch (botMetaData.getBotLanguage()){
            case JAVA:
                return new JavaBotRunner(botMetaData, timoutInMilis);
            case PYTHON:
                return new PythonBotRunner(botMetaData, timoutInMilis);
            case CSHARP:
                return new CSharpCoreBotRunner(botMetaData, timoutInMilis);
            case JAVASCRIPT:
                return new JavaScriptBotRunner(botMetaData, timoutInMilis);
            default:
                break;
        }
        throw new InvalidRunnerState("Invalid bot language");
    }
}
