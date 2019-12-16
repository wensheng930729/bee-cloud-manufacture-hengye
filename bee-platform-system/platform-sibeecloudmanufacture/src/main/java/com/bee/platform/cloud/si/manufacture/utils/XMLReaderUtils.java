package com.bee.platform.cloud.si.manufacture.utils;

import com.bee.platform.cloud.si.manufacture.entity.ElectricityMeterData;
import com.bee.platform.common.utils.DateUtils;
import org.dom4j.Attribute;
import org.dom4j.Document;
import org.dom4j.Element;
import org.springframework.util.ObjectUtils;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.util.Iterator;
import java.util.List;

/**
 * @ClassName: XMLReaderUtils
 * @Description: java类作用描述
 * @Author: fei.sun
 * @Date: 2019/10/25 10:06
 * @Version: 1.0
 */
public class XMLReaderUtils {

    /**
     * 解析xml中的数据
     * @param doc d
     * @throws Exception e
     */
    public static ElectricityMeterData read(Document doc) throws Exception {
        //获取根节点
        Element rootElement = doc.getRootElement();
        Element commonElement = rootElement.element("common");
        String buildingId = commonElement.element("building_id").getStringValue();
        String gatewayId = commonElement.element("gateway_id").getStringValue();
        Element dataElement = rootElement.element("data");
        String time = dataElement.element("time").getTextTrim();
        Element meterElement = dataElement.element("meter");
        String id = meterElement.attributeValue("id");
        //
        ElectricityMeterData electricityMeterData = new ElectricityMeterData();
        electricityMeterData.setBuilding_id(buildingId).setGateway_id(gatewayId)
                .setEquipment_id(id).setTime(DateUtils.parse(time,"yyyyMMddHHmmss"));
        Iterator iterator = meterElement.elementIterator("function");
        while (iterator.hasNext()){
            Element element = (Element) iterator.next();
            String coding = element.attributeValue("coding");
            String stringValue = element.getStringValue();
            String replaceCoding = coding.replace("-", "_");
            Field declaredField = electricityMeterData.getClass().getDeclaredField(replaceCoding);
            if(!ObjectUtils.isEmpty(declaredField)){
                declaredField.setAccessible(true);
                declaredField.set(electricityMeterData,Double.valueOf(stringValue));
            }
        }
        return electricityMeterData;
    }

    public static void main(String[] args) throws Exception {
        ElectricityMeterData electricityMeterData = new ElectricityMeterData();
        Field yc_001 = ElectricityMeterData.class.getDeclaredField("yc_001");
        yc_001.setAccessible(true);
        yc_001.set(electricityMeterData,0.88);
        System.out.println(electricityMeterData);
    }
}
