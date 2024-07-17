package info.openmeta.starter.metadata.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * SysTranslation Model
 */
@Data
@Schema(name = "SysTranslation")
@EqualsAndHashCode(callSuper = true)
public class SysTranslation extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "Language Code")
    private String languageCode;

    @Schema(description = "Original")
    private String original;

    @Schema(description = "Translation")
    private String translation;

    @Schema(description = "Disabled")
    private Boolean disabled;
}