package info.openmeta.framework.orm.domain;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class TestEntity {
    private String name;
    private Integer value;
}
