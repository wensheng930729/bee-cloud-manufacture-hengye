package com.bee.platform.cloud.si.manufacture.entity;

import java.io.Serializable;

import org.springframework.data.annotation.Id;
import org.springframework.data.elasticsearch.annotations.DateFormat;
import org.springframework.data.elasticsearch.annotations.Document;
import org.springframework.data.elasticsearch.annotations.Field;
import org.springframework.data.elasticsearch.annotations.FieldType;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
@NoArgsConstructor
@Data
@Accessors(chain = true)
@Document(indexName = "index_ifms_eszelecmeter_data", type = "eszelecmeter_data" , shards = 4, replicas = 1)
public class ESZElecMeterData implements Serializable {
    /**
     * 主键id
     * 表字段 : z_elec_meter_data.id
     */
    private Integer id;
	
    /**
     * 工厂id
     * 表字段 : z_elec_meter_data.factory_id
     */
    private Integer factoryId;

    /**
     * 客户端id
     * 表字段 : z_elec_meter_data.api_client_id
     */
    private String apiClientId;

    /**
     * com端口
     * 表字段 : z_elec_meter_data.chanel_id
     */
    private String chanelId;

    /**
     * 电表id
     * 表字段 : z_elec_meter_data.device_id
     */
    private String deviceId;

    /**
     * 读表时间
     * 表字段 : z_elec_meter_data.meter_read_time
     */
    private Long meterReadTime;

    /**
     * 总有功电能
     * 表字段 : z_elec_meter_data.EP
     */
    private Float ep;

    /**
     * 总输入有功电能
     * 表字段 : z_elec_meter_data.EP_in
     */
    private Float epIn;

    /**
     * 总输出有功电能
     * 表字段 : z_elec_meter_data.EP_out
     */
    private Float epOut;

    /**
     * 总无功电能
     * 表字段 : z_elec_meter_data.EQ
     */
    private Float eq;

    /**
     * 总出入无功电能
     * 表字段 : z_elec_meter_data.EQ_in
     */
    private Float eqIn;

    /**
     * 总输出无功电能
     * 表字段 : z_elec_meter_data.EQ_out
     */
    private Float eqOut;

    /**
     * 频率
     * 表字段 : z_elec_meter_data.F
     */
    private Float f;

    /**
     * A相电流
     * 表字段 : z_elec_meter_data.Ia
     */
    private Float ia;

    /**
     * B相电流
     * 表字段 : z_elec_meter_data.Ib
     */
    private Float ib;

    /**
     * C相电流
     * 表字段 : z_elec_meter_data.Ic
     */
    private Float ic;

    /**
     * 总有功功率
     * 表字段 : z_elec_meter_data.P
     */
    private Float p;

    /**
     * A相有功功率
     * 表字段 : z_elec_meter_data.Pa
     */
    private Float pa;

    /**
     * B相有功功率
     * 表字段 : z_elec_meter_data.Pb
     */
    private Float pb;

    /**
     * C相有功功率
     * 表字段 : z_elec_meter_data.Pc
     */
    private Float pc;

    /**
     * 总功率因数
     * 表字段 : z_elec_meter_data.PF
     */
    private Float pf;

    /**
     * A相功率因数
     * 表字段 : z_elec_meter_data.PFa
     */
    private Float pfa;

    /**
     * B相功率因数
     * 表字段 : z_elec_meter_data.PFb
     */
    private Float pfb;

    /**
     * C相功率因数
     * 表字段 : z_elec_meter_data.PFc
     */
    private Float pfc;

    /**
     * 总无功功率
     * 表字段 : z_elec_meter_data.Q
     */
    private Float q;

    /**
     * A相无功功率
     * 表字段 : z_elec_meter_data.Qa
     */
    private Float qa;

    /**
     * B相无功功率
     * 表字段 : z_elec_meter_data.Qb
     */
    private Float qb;

    /**
     * C相无功功率
     * 表字段 : z_elec_meter_data.Qc
     */
    private Float qc;

    /**
     * 总视在功率
     * 表字段 : z_elec_meter_data.S
     */
    private Float s;

    /**
     * A相电压
     * 表字段 : z_elec_meter_data.Ua
     */
    private Float ua;

    /**
     * B相电压
     * 表字段 : z_elec_meter_data.Ub
     */
    private Float ub;

    /**
     * C相电压
     * 表字段 : z_elec_meter_data.Uc
     */
    private Float uc;

    /**
     * 总有功电度
     * 表字段 : z_elec_meter_data.Wp
     */
    private Float wp;

    /**
     * 0,未删除，1，已删除
     * 表字段 : z_elec_meter_data.logic_delete
     */
    private Integer logicDelete;

    private static final long serialVersionUID = 1L;

   
}