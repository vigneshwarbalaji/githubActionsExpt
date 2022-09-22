package com.yoco.commons.config;

import freemarker.cache.ClassTemplateLoader;
import freemarker.template.Configuration;
import freemarker.template.TemplateException;
import freemarker.template.Version;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.util.Map;

@Slf4j
@NoArgsConstructor
public class TemplateConfig {

    private static Configuration config;

    static {
        configureTemplates();
        loadTemplates();
    }

    private static void configureTemplates(){
        config = new Configuration(new Version(Configuration.VERSION_2_3_0.toString()));
        config.setDefaultEncoding(StandardCharsets.UTF_8.toString());
    }

    private static void loadTemplates(){
        config.setTemplateLoader(new ClassTemplateLoader(TemplateConfig.class, "/templates"));
    }

    public static String processAndExtractTemplate(String templateName, Map<String,Object> model) throws TemplateException, IOException {
        var template = config.getTemplate(templateName);
        var stringWriter = new StringWriter();
        template.process(model,stringWriter);
        return stringWriter.getBuffer().toString();
    }

    public Configuration getConfig(){
        return config;
    }

}
