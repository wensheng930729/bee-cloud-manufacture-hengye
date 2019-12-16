package com.bee.platform.cloud.si.manufacture.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.enums.IdType;
import java.math.BigDecimal;
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
 * 料批基本信息主表
 * </p>
 *
 * @author huangxin123
 * @since 2019-09-24
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class ProMaterialBatch extends Model<ProMaterialBatch> {

    private static final long serialVersionUID = 1L;

    /**
     * 料批id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Long id;
    /**
     * 业务id
     */
    private String businessId;
    /**
     * 料批名称
     */
    private String materialName;
    /**
     * plc设备名称
     */
    private String plcName;
    /**
     * plc设备id
     */
    private Integer plcId;
    /**
     * 成品id
     */
    private Integer finishProductId;
    /**
     * 成品名称
     */
    private String finishProductName;
    /**
     * 生产数量
     */
    private BigDecimal productionNum;
    /**
     * 单位
     */
    private String finishUnit;
    /**
     * 状态：1启用，0停用
     */
    private Integer active;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 更新时间
     */
    private Date modifyTime;
    /**
     * 创建人id
     */
    private Integer createId;
    /**
     * 修改人id
     */
    private Integer modifyId;
    /**
     * 工厂id
     */
    private Integer factoryId;
    /**
     * 企业id
     */
    private Integer companyId;
    /**
     * 数据状态:0删除1正常
     */
    private Integer status;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
