package com.bee.platform.cloud.si.manufacture.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.Version;

import lombok.*;
import lombok.experimental.Accessors;

/**
 * <p>
 * 打印机码表
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-09-30
 */
@NoArgsConstructor
@Setter
@Getter
@Accessors(chain=true)
@ToString
@TableName("config_printer_token")
public class ConfigPrinterToken extends Model<ConfigPrinterToken> {


    private static final long serialVersionUID = 3513620031594912512L;
    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 所属企业id
     */
    private Integer enterpriseId;
    /**
     * 工厂id
     */
    private Integer factoryId;
    /**
     * 机器编号
     */
    private String appId;
    /**
     * app_key
     */
    private String appKey;
    /**
     * 打印设备id
     */
    private String deviceId;
    /**
     * 地磅设备id
     */
    private String printerId;
    /**
     * 打印模板id
     */
    private String templateId;
    /**
     * token
     */
    private String accessToken;
    /**
     * token生成时间
     */
    private Date createTime;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
