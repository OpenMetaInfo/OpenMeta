package info.openmeta.framework.orm.domain;

import info.openmeta.framework.base.utils.SFunction;
import info.openmeta.framework.orm.utils.LambdaUtils;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * SubQueries object, used to specify the subQuery for different relational fields.
 * Example:
 *     1. ManyToOne/OneToOne: new SubQueries().expand(User::getDept)
 *     2. OneToMany/ManyToMany: new SubQueries().expand(User::getRoles)
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class SubQueries {

    // SubQuery map, key is the field name, value is the SubQuery object
    private Map<String, SubQuery> queryMap = new HashMap<>();

    /**
     * Update the subQuery conditions based on the ManyToOne/OneToOne fields in the cascade fields,
     * that is, the fields of the associated model to be read in the cascade.
     * Example:
     *      1. ManyToOne/OneToOne: new SubQueries().expand(User::getDept)
     *      2. OneToMany/ManyToMany: new SubQueries().expand(User::getRoles)
     *
     * @param method Field lambda method reference
     */
    public <T, R> SubQueries expand(SFunction<T, R> method) {
        String field = LambdaUtils.getAttributeName(method);
        this.queryMap.put(field, new SubQuery());
        return this;
    }

    /**
     * Update the subQuery conditions based on the ManyToOne/OneToOne fields in the cascade fields,
     * that is, the fields of the associated model to be read in the cascade.
     *
     * @param method Field lambda method reference
     * @param fields Field set of the associated model to be read in the cascade
     */
    public <T, R> SubQueries expand(SFunction<T, R> method, List<String> fields) {
        String field = LambdaUtils.getAttributeName(method);
        this.queryMap.put(field, new SubQuery(fields));
        return this;
    }

    /**
     * Set SubQuery conditions
     *
     * @param field Field name
     * @param subQuery SubQuery condition
     */
    public SubQueries expand(String field, SubQuery subQuery) {
        this.queryMap.put(field, subQuery);
        return this;
    }

}
