package za.co.entelect.challenge.bootstrapper;


import com.google.gson.annotations.SerializedName;
import za.co.entelect.challenge.entities.BotMetaData;

public class Config {
    @SerializedName("bot-meta")
    public BotMetaData[] botMetaData;

    @SerializedName("console-players")
    public int consolePlayers;

    @SerializedName("max-runtime-ms")
    public int maximumBotRuntimeMiliSeconds;

    @SerializedName("round-state-output-location")
    public String roundStateOutputLocation;

}
