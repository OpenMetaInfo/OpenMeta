package info.openmeta.framework.orm.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;

/**
 * Model reference object.
 * Used to reference the model row ID and display name.
 */
@Data
public class ModelReference implements Serializable  {

    @Serial
    private static final long serialVersionUID = 1L;

    @Schema(description = "Model row ID")
    private Serializable id;

    @Schema(description = "Display Name")
    private String displayName;

    /**
     * Create a ModelReference object.
     *
     * @param id model row ID
     * @param displayName Display Name
     * @return ModelReference object
     */
    static public ModelReference of(Serializable id, String displayName) {
        ModelReference modelReference = new ModelReference();
        modelReference.setId(id);
        modelReference.setDisplayName(displayName);
        return modelReference;
    }
}
