package info.openmeta.starter.metadata.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * SysOptionSetTrans Model
 */
@Data
@Schema(name = "SysOptionSetTrans")
@EqualsAndHashCode(callSuper = true)
public class SysOptionSetTrans extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    private Long id;

    @Schema(description = "Language Code")
    private String languageCode;

    @Schema(description = "Row ID")
    private Long rowId;

    @Schema(description = "Option Set Name")
    private String name;

    @Schema(description = "Description")
    private String description;
}