package za.co.entelect.challenge.botrunners;

import org.apache.commons.exec.CommandLine;
import org.apache.commons.exec.DefaultExecutor;
import org.apache.commons.exec.ExecuteWatchdog;
import org.apache.commons.exec.PumpStreamHandler;
import za.co.entelect.challenge.entities.BotMetaData;
import za.co.entelect.challenge.utils.FileUtils;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;

public abstract class BotRunner {
    protected BotMetaData botMetaData;
    protected int timeoutInMilliseconds;

    protected BotRunner(BotMetaData botMetaData, int timeoutInMilliseconds){
        this.botMetaData = botMetaData;
        this.timeoutInMilliseconds = timeoutInMilliseconds;
    }

    public String run() throws IOException{
        return this.runBot();
    }

    protected abstract String runBot() throws IOException;

    public String getBotDirectory(){
        return FileUtils.getAbsolutePath(botMetaData.getBotLocation());
    }

    public String getBotFileName(){ return botMetaData.getBotFileName(); }

    protected String RunSimpleCommandLineCommand(String line, int expectedExitValue) throws IOException {
        CommandLine cmdLine = CommandLine.parse(line);
        DefaultExecutor executor = new DefaultExecutor();
        executor.setWorkingDirectory(new File(this.getBotDirectory()));
        executor.setExitValue(expectedExitValue);

        ExecuteWatchdog watchdog = new ExecuteWatchdog(this.timeoutInMilliseconds);
        executor.setWatchdog(watchdog);

        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        PumpStreamHandler streamHandler = new PumpStreamHandler(outputStream);
        executor.setStreamHandler(streamHandler);
        executor.execute(cmdLine);
        return outputStream.toString();
    }
}
