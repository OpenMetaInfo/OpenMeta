package info.openmeta.framework.web.service;

import java.util.Map;

public interface TempTokenService {

    String saveTempToken(String token, int hours, Map<String, Object> variables);

    String parseTempToken(String token);

    Map<String, String> parseTempToken(String type, String token);

    String generateTempToken(Long id);

    String generateTempToken(String type, Long id, int hours);

    String generateTempToken(Map<String, Long> objects, int hours);

    String generateTempToken(String type, Object obj, int minutes);
}
