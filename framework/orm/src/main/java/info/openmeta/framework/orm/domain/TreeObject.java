package info.openmeta.framework.orm.domain;

import info.openmeta.framework.orm.entity.BaseModel;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.util.List;

/**
 * Tree objects
 */
@Data
public class TreeObject<T extends BaseModel> implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    private T parent;

    private List<T> children;
}
