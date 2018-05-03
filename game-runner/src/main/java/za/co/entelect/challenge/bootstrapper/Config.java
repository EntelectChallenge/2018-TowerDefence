package za.co.entelect.challenge.bootstrapper;
import com.google.gson.annotations.SerializedName;

public class Config {
    @SerializedName("player-a")
    public String playerAConfig;

    @SerializedName("player-b")
    public String playerBConfig;

    @SerializedName("max-runtime-ms")
    public int maximumBotRuntimeMilliSeconds;

    @SerializedName("round-state-output-location")
    public String roundStateOutputLocation;

}
