package info.openmeta.starter.designer.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * DesignFieldTrans Model
 */
@Data
@Schema(name = "DesignFieldTrans")
@EqualsAndHashCode(callSuper = true)
public class DesignFieldTrans extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    private Long id;

    @Schema(description = "Language Code")
    private String languageCode;

    @Schema(description = "Row ID")
    private Long rowId;

    @Schema(description = "Label Name")
    private String labelName;

    @Schema(description = "Description")
    private String description;
}