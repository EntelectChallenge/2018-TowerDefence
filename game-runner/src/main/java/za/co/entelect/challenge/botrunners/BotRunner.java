package za.co.entelect.challenge.botrunners;

import org.apache.commons.exec.CommandLine;
import org.apache.commons.exec.DefaultExecutor;
import org.apache.commons.exec.ExecuteWatchdog;
import za.co.entelect.challenge.entities.BotMetaData;
import za.co.entelect.challenge.utils.FileUtils;

import java.io.File;
import java.io.IOException;

public abstract class BotRunner {
    protected BotMetaData botMetaData;
    protected int timoutInMilis;

    protected BotRunner(BotMetaData botMetaData, int timoutInMilis){
        this.botMetaData = botMetaData;
        this.timoutInMilis = timoutInMilis;
    }

    public int run() throws IOException{
        return this.runBot();
    }

    protected abstract int runBot() throws IOException;

    public String getBotDirectory(){
        return FileUtils.getAbsolutePath(botMetaData.getBotLocation());
    }

    public String getBotFileName(){ return botMetaData.getBotFileName(); }

    protected int RunSimpleCommandLineCommand(String line, int expectedExitValue) throws IOException {
        CommandLine cmdLine = CommandLine.parse(line);
        DefaultExecutor executor = new DefaultExecutor();
        executor.setWorkingDirectory(new File(this.getBotDirectory()));
        executor.setExitValue(expectedExitValue);
        ExecuteWatchdog watchdog = new ExecuteWatchdog(this.timoutInMilis);
        executor.setWatchdog(watchdog);
        return executor.execute(cmdLine);
    }
}
