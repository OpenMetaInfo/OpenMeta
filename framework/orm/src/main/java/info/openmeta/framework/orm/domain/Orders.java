package info.openmeta.framework.orm.domain;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.base.utils.Cast;
import info.openmeta.framework.base.utils.SFunction;
import info.openmeta.framework.orm.domain.serializer.OrdersDeserializer;
import info.openmeta.framework.orm.domain.serializer.OrdersSerializer;
import info.openmeta.framework.orm.utils.LambdaUtils;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.util.CollectionUtils;

import java.util.*;
import java.util.stream.Collectors;


/**
 * Order conditions, support two input formats:
 * Pure string format: "name ASC, sequence DESC"
 * List object format: [["name", "ASC"], ["sequence", "DESC"]]
 */
@Getter
@NoArgsConstructor
@JsonSerialize(using = OrdersSerializer.class)
@JsonDeserialize(using = OrdersDeserializer.class)
@Schema(type = "array",
        example = "[\"name\", \"ASC\"]",
        description = """
                Support multiple order conditions:
                * []
                * ["name", "ASC"]
                * [["name", "ASC"], ["sequence", "DESC"]]
                * or string format: "name ASC, sequence DESC"
                """
)
public class Orders {
    public static final String ASC = "ASC";
    public static final String DESC = "DESC";
    private static final String DEFAULT_ORDER = "ASC";

    @Schema(hidden = true)
    private final Set<String> fields = new HashSet<>();

    @Schema(hidden = true)
    private final List<List<String>> orderList = new ArrayList<>();

    /**
     * Order condition check
     * @param field field name
     * @param order order
     * @return Orders object
     */
    private Orders add(String field, String order) {
        if (ASC.equals(order)) {
            this.orderList.add(Arrays.asList(field, ASC));
        } else if (DESC.equals(order)) {
            this.orderList.add(Arrays.asList(field, DESC));
        } else {
            throw new IllegalArgumentException("Ordering condition exception: {0} {1}", field, order);
        }
        this.fields.add(field);
        return this;
    }

    /**
     * Static initialization method
     * @param field field name
     * @return Orders object
     */
    public static Orders ofAsc(String field) {
        Orders orders = new Orders();
        orders.add(field, ASC);
        return orders;
    }

    /**
     * Static initialization method
     * @param method field method, Lambda expression, method reference passing parameters
     * @return Orders object
     */
    public static <T, R> Orders ofAsc(SFunction<T, R> method) {
        String field = LambdaUtils.getAttributeName(method);
        return ofAsc(field);
    }

    /**
     * Add a positive sort field
     * @param field field name
     * @return Orders object
     */
    public Orders addAsc(String field) {
        return add(field, ASC);
    }

    /**
     * Add a positive sort field
     * @param method field method, Lambda expression, method reference passing parameters
     * @return Orders object
     */
    public <T, R> Orders addAsc(SFunction<T, R> method) {
        String field = LambdaUtils.getAttributeName(method);
        return addAsc(field);
    }

    /**
     * Constructor for reverse order sorting
     * @param field field name
     * @return Orders object
     */
    public static Orders ofDesc(String field) {
        Orders orders = new Orders();
        orders.add(field, DESC);
        return orders;
    }

    /**
     * Constructor for reverse order sorting
     * @param method field method, Lambda expression, method reference passing parameters
     * @return Orders object
     */
    public static <T, R> Orders ofDesc(SFunction<T, R> method) {
        String field = LambdaUtils.getAttributeName(method);
        return ofDesc(field);
    }

    /**
     * Add a reverse sort field
     * @param field field name
     * @return Orders object
     */
    public Orders addDesc(String field) {
        return add(field, DESC);
    }

    /**
     * Add a reverse sort field
     * @param method field method, Lambda expression, method reference passing parameters
     */
    public <T, R> Orders addDesc(SFunction<T, R> method) {
        String field = LambdaUtils.getAttributeName(method);
        return addDesc(field);
    }

    /**
     * Orders string parsing, remove leading and trailing spaces from the string
     * @param ordersString Sorting string, such as "name ASC, sequence DESC"
     * @return Orders object
     */
    public static Orders of(String ordersString) {
        if (StringUtils.isBlank(ordersString)) {
            return null;
        }
        Orders orders = new Orders();
        for (String orderUnit : StringUtils.split(ordersString, ",")) {
            orders.addStringOrderUnit(orderUnit);
        }
        return orders;
    }

    /**
     * Orders string list parsing.
     * Compatible with [], ["name", "ASC"], and [["name", "ASC"], ["sequence", "DESC"]].
     *
     * @param orderList Sorting string list
     * @return Orders object
     */
    public static Orders of(List<Object> orderList) {
        if (CollectionUtils.isEmpty(orderList)) {
            return null;
        }
        Orders orders = new Orders();
        if (orderList.getFirst() instanceof String) {
            orders.addOrderUnit(Cast.of(orderList));
        } else {
            orderList.forEach(unit -> orders.addOrderUnit(Cast.of(unit)));
        }
        return orders;
    }

    /**
     * Add a string sorting condition to the Orders object
     * @param orderUnit The smallest sorting unit, such as 'name ASC'
     */
    private void addStringOrderUnit(String orderUnit) {
        String[] s = StringUtils.split(orderUnit, " ");
        if (s.length == 1) {
            this.add(s[0].trim(), DEFAULT_ORDER);
        } else if (s.length == 2) {
            this.add(s[0].trim(), s[1].toUpperCase().trim());
        } else if (s.length > 2) {
            throw new IllegalArgumentException("Sorting parameter exception: {0}", orderUnit);
        }
    }

    /**
     * Add a field sorting condition to the Orders object
     * @param unit The smallest sorting unit, such as ["name", "ASC"]
     */
    private void addOrderUnit(List<String> unit) {
        if (unit.size() == 1) {
            this.add(unit.getFirst().trim(), DEFAULT_ORDER);
        } else if (unit.size() == 2) {
            this.add(unit.get(0).trim(), unit.get(1).toUpperCase().trim());
        } else if (unit.size()  > 2) {
            throw new IllegalArgumentException("Sorting parameter exception: {0}", unit);
        }
    }

    @Override
    public String toString() {
        return this.orderList.stream()
                .map(pair -> pair.getFirst() + " " + pair.get(1))
                .collect(Collectors.joining(", "));
    }
}
