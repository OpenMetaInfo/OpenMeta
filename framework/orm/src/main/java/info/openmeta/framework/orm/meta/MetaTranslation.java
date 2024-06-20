package info.openmeta.framework.orm.meta;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * Translation Metadata
 */
@Data
@Schema(name = "Translation Metadata")
public class MetaTranslation implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "ID")
    private Long id;

    @Schema(description = "Language Code")
    private String languageCode;

    @Schema(description = "Original")
    private String original;

    @Schema(description = "Translation")
    private String translation;
}