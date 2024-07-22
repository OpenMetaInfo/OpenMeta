package info.openmeta.starter.metadata.message.dto;

import info.openmeta.framework.base.context.Context;
import info.openmeta.starter.metadata.enums.MetadataReloadType;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Broadcast message for reloading metadata
 */
@Data
@NoArgsConstructor
public class ReloadMetadataMessage {

    private String model;
    private MetadataReloadType reloadType;

    private Context context;

    public ReloadMetadataMessage(String model, MetadataReloadType reloadType, Context context) {
        this.model = model;
        this.reloadType = reloadType;
        this.context = context;
    }
}
