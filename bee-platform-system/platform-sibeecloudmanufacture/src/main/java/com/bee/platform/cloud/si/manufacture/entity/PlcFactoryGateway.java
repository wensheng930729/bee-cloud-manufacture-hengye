package com.bee.platform.cloud.si.manufacture.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.enums.IdType;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.Version;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

/**
 * <p>
 * plc 工厂和华辰智通网关的配置
 * </p>
 *
 * @author MP123
 * @since 2019-10-17
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class PlcFactoryGateway extends Model<PlcFactoryGateway> {

    private static final long serialVersionUID = 1L;

    /**
     * 主键
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 华辰智通网关id
     */
    private String hcGatewayId;
    /**
     * 网关设备名称
     */
    private String hcGatewayName;
    /**
     * 工厂id
     */
    private Integer factoryId;
    /**
     * 是否有效 0 无效 ，1 有效
     */
    private Integer status;
    /**
     * 是否删除
     */
    private Integer deleted;
    /**
     * 创建人id
     */
    private Integer createId;
    /**
     * 创建人名称
     */
    private String creator;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 修改人id
     */
    private Integer modifyId;
    /**
     * 修改人名称
     */
    private String modifier;
    /**
     * 修改时间
     */
    private Date modifyTime;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

    public PlcFactoryGateway() {
    }

    public PlcFactoryGateway(String hcGatewayId, String hcGatewayName, Integer factoryId, Integer status) {
        this.hcGatewayId = hcGatewayId;
        this.hcGatewayName = hcGatewayName;
        this.factoryId = factoryId;
        this.status = status;
    }
}
