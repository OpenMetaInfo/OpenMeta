package info.openmeta.framework.orm.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import info.openmeta.framework.base.utils.Assert;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * View type Enum.
 */
@Getter
@AllArgsConstructor
public enum ViewType {
    TABLE("Table", "Table"),
    FORM("Form", "Form"),
    CARD("Card", "Card"),
    KANBAN("Kanban", "Kanban"),
    CALENDAR("Calendar", "Calendar"),
    DASHBOARD("Dashboard", "Dashboard"),
    ;

    @JsonValue
    private final String type;
    private final String name;

    /** type map */
    static private final Map<String, ViewType> TYPE_MAP = Stream.of(values()).collect(Collectors.toMap(ViewType::getType, Function.identity()));

    /**
     * Get ViewType by string
     * @param type string
     * @return ViewType
     */
    public static ViewType of(String type) {
        Assert.isTrue(TYPE_MAP.containsKey(type), "{0} not exist in ViewType!", type);
        return TYPE_MAP.get(type);
    }
}
