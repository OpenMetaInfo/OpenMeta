package info.openmeta.framework.orm.model;

import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * SysModelTrans Model
 */
@Data
@Schema(name = "SysModelTrans")
@EqualsAndHashCode(callSuper = true)
public class SysModelTrans extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "Language Code")
    private String languageCode;

    @Schema(description = "Row ID")
    private Long rowId;

    @Schema(description = "Label Name")
    private String labelName;

    @Schema(description = "Description")
    private String description;
}