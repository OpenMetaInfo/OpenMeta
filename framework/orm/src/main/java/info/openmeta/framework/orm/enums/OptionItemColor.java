package info.openmeta.framework.orm.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum OptionItemColor {
    LIGHT_GREY("LightGrey", "Light Grey"),
    LIGHT_BLUE("LightBlue", "Light Blue"),
    LIGHT_GREEN("LightGreen", "Light Green"),
    LIGHT_YELLOW("LightYellow", "Light Yellow"),
    LIGHT_RED("LightRed", "Light Red"),
    LIGHT_PURPLE("LightPurple", "Light Purple"),
    DARK_GREY("DarkGrey", "Dark Grey"),
    DARK_BLUE("DarkBlue", "Dark Blue"),
    DARK_GREEN("DarkGreen", "Dark Green"),
    DARK_YELLOW("DarkYellow", "Dark Yellow"),
    DARK_RED("DarkRed", "Dark Red"),
    DARK_PURPLE("DarkPurple", "Dark Purple")
    ;

    @JsonValue
    private final String code;
    private final String name;
}
