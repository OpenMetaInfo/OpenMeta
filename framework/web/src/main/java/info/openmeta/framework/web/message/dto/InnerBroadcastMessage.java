package info.openmeta.framework.web.message.dto;

import info.openmeta.framework.base.context.Context;
import info.openmeta.framework.web.message.enums.InnerBroadcastType;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Inner broadcast message
 */
@Data
@NoArgsConstructor
public class InnerBroadcastMessage {

    private InnerBroadcastType broadcastType;

    private Context context;

}
