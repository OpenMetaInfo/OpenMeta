package info.openmeta.starter.metadata.message.dto;

import info.openmeta.framework.base.context.Context;
import info.openmeta.starter.metadata.message.enums.InnerBroadcastType;
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
