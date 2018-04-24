package za.co.entelect.challenge.utils;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

public final class FileUtils {

    public static void writeToFile(String fileLocation, String stringToWrite)  throws IOException {
        BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter(new File(fileLocation)));
        bufferedWriter.write(stringToWrite);
        bufferedWriter.flush();
        bufferedWriter.close();
    }

    public static String getAbsolutePath(String path){
        return new File(path).getAbsolutePath();
    }

    public static String getRoundDirectory(int roundNumber){
       return String.format("Round %03d",roundNumber);
    }
}
