package info.openmeta.starter.metadata.entity;

import info.openmeta.framework.orm.entity.TranslationModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * SysFieldTrans Model
 */
@Data
@Schema(name = "SysFieldTrans")
@EqualsAndHashCode(callSuper = true)
public class SysFieldTrans extends TranslationModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "Label Name")
    private String labelName;

    @Schema(description = "Description")
    private String description;
}