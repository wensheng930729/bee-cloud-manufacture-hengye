package com.bee.platform.cloud.si.manufacture.entity;

import lombok.Data;
import lombok.experimental.Accessors;
import org.springframework.data.annotation.Id;
import org.springframework.data.elasticsearch.annotations.Document;
import org.springframework.data.elasticsearch.annotations.Field;
import org.springframework.data.elasticsearch.annotations.FieldType;

import java.math.BigDecimal;
import java.util.Date;

/**
 * @ClassName: ElectricityMeterData
 * @Description: 电表数据
 * @Author: fei.sun
 * @Date: 2019/10/25 11:14
 * @Version: 1.0
 */
@Data
@Accessors(chain = true)
@Document(indexName = "electricity_meter_data", type = "electricity_meter_data" , shards = 4, replicas = 1)
public class ElectricityMeterData {

    @Id
    private String id;
    /**
     * 楼栋号
     */
    @Field(type = FieldType.Keyword)
    private String building_id;
    /**
     * 网关号
     */
    @Field(type = FieldType.Keyword)
    private String gateway_id;
    /**
     * 采集时间
     */
    @Field(type = FieldType.Date)
    private Date time;
    /**
     *设备标识
     */
    @Field(type = FieldType.Keyword)
    private String equipment_id;
    /**
     * 字段1
     */
    @Field(type = FieldType.Double)
    private Double yc_001;
    /**
     * 字段1
     */
    @Field(type = FieldType.Double)
    private Double yc_002;
    /**
     * 字段1
     */
    @Field(type = FieldType.Double)
    private Double yc_003;
    /**
     * 字段1
     */
    @Field(type = FieldType.Double)
    private Double yc_004;
    /**
     * 字段1
     */
    @Field(type = FieldType.Double)
    private Double yc_005;
    /**
     * 字段1
     */
    @Field(type = FieldType.Double)
    private Double yc_006;
    /**
     * 字段1
     */
    @Field(type = FieldType.Double)
    private Double yc_007;
    /**
     * 字段1
     */
    @Field(type = FieldType.Double)
    private Double yc_008;

}
