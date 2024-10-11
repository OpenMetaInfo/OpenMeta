package info.openmeta.framework.orm.entity;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;

/**
 * Abstract class of translation model.
 */
@Data
@EqualsAndHashCode(callSuper = true)
public abstract class TranslationModel extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "Language Code")
    private String languageCode;

    @Schema(description = "Row ID")
    private Long rowId;

}
