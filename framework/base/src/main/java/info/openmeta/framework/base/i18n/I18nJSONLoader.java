package info.openmeta.framework.base.i18n;

import com.fasterxml.jackson.core.type.TypeReference;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.base.exception.JSONException;
import info.openmeta.framework.base.utils.JsonMapper;
import org.springframework.boot.CommandLineRunner;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.List;

/**
 * Load the i18n translations.
 */
@Component
public class I18nJSONLoader implements CommandLineRunner {

    private static final String LOCATION_PATTERN = "classpath*:i18n/*.json";

    @Override
    public void run(String... args) throws Exception {
        PathMatchingResourcePatternResolver resolver = new PathMatchingResourcePatternResolver();
        try {
            Resource[] resources = resolver.getResources(LOCATION_PATTERN);
            for (Resource resource : resources) {
                String filename = resource.getFilename();
                if (filename == null) {
                    continue;
                }
                String languageCode = getLanguageCode(filename);
                String content = new String(resource.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
                if (content.isBlank() || content.equals("{}") || content.equals("[]")) {
                    continue;
                }
                try {
                    List<TranslationItem> translationItems = JsonMapper.stringToObject(content, new TypeReference<>() {});
                    I18n.put(languageCode, translationItems);
                } catch (JSONException e) {
                    throw new IllegalArgumentException("Invalid i18n JSON file: {0}", filename, e);
                }
            }
        } catch (IOException e) {
            throw new RuntimeException("Failed to load i18n JSON files", e);
        }
    }

    /**
     * Get language code from the filename.
     * The filename should be like: i18n/messages.en_US.json
     */
    private String getLanguageCode(String filename) {
        int firstDotIndex = filename.indexOf(".");
        int lastDotIndex = filename.lastIndexOf(".");
        if (firstDotIndex == -1 || firstDotIndex >= lastDotIndex - 1) {
            throw new IllegalArgumentException("Language code is required in the filename: " + filename);
        }
        return filename.substring(firstDotIndex + 1, lastDotIndex);
    }

}
