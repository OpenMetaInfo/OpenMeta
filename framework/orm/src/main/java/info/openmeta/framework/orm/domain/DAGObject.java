package info.openmeta.framework.orm.domain;

import info.openmeta.framework.orm.entity.BaseModel;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.util.List;


/**
 * DAG objects
 * @param <T>
 */
@Data
public class DAGObject<T extends BaseModel> implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    private T parent;

    private List<T> children;
}
