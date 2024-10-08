package info.openmeta.starter.metadata.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * SysOptionItemTrans Model
 */
@Data
@Schema(name = "SysOptionItemTrans")
@EqualsAndHashCode(callSuper = true)
public class SysOptionItemTrans extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "Item Name")
    private String itemName;

    @Schema(description = "Description")
    private String description;
}