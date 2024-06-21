package info.openmeta.framework.orm.changelog.message.dto;

import info.openmeta.framework.base.context.Context;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Map;

/**
 * Common Log Message
 */
@Data
@NoArgsConstructor
public class CommonLogMessage {

    private String model;
    private Map<String, Object> data;

    private Context context;

    public CommonLogMessage(String model, Map<String, Object> data, Context context) {
        this.model = model;
        this.data = data;
        this.context = context;
    }
}
