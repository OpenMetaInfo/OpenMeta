package info.openmeta.framework.orm.jdbc.database;

import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * SQL operation parameters, including SQL statement and parameter value list
 */
@Data
@NoArgsConstructor
public class SqlParams {
    private String sql;
    private List<Object> args = new ArrayList<>();

    public SqlParams(String sql) {
        this.sql = sql;
    }

    public void addArgValue(Object value) {
        args.add(value);
    }

    public Object[] getArgsArray() {
        return args.toArray(new Object[0]);
    }

    /**
     * Equivalent SQL statement, using parameters to replace placeholders, only for log output!
     *
     * @return Complete SQL statement
     */
    public String toLogString() {
        if (args.isEmpty()) {
            return sql;
        }
        Matcher m = Pattern.compile("\\?").matcher(sql);
        StringBuilder sb = new StringBuilder();
        int i = 0;
        while (m.find() && i < args.size()) {
            if (args.get(i) == null) {
                m.appendReplacement(sb, "null");
            } else if (args.get(i) instanceof String) {
                String replacement = Matcher.quoteReplacement(args.get(i).toString());
                m.appendReplacement(sb, "'" + replacement + "'");
            } else {
                m.appendReplacement(sb, args.get(i).toString());
            }
            i++;
        }
        m.appendTail(sb);
        return sb.toString();
    }
}
