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
 * 成品装袋
 * </p>
 *
 * @author MP123
 * @since 2019-10-09
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class ProBagging extends Model<ProBagging> {

    private static final long serialVersionUID = 1L;

    /**
     * 主键id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Long id;
    /**
     * 企业id
     */
    private Integer enterpriseId;
    /**
     * 工厂id
     */
    private Integer factoryId;
    /**
     * 吨袋编号
     */
    private String baggingCode;
    /**
     * 样品编号（磅单取样标准的样品编号）
     */
    private String sampleCode;
    /**
     * 成品id
     */
    private Integer productId;
    /**
     * 成品名称
     */
    private String productName;
    /**
     * 炉号id
     */
    private Integer furnaceId;
    /**
     * 炉号名称
     */
    private String furnaceName;
    /**
     * 班次编码
     */
    private Integer  shiftCode;
    /**
     * 班次:1一班，2二班，3三班
     */
    private  String shift;
    /**
     * 班次时间
     */
    private Date shiftTime;
    /**
     * 出炉批次编号
     */
    private Integer furnaceBatchCode;
    /**
     * 出炉批次
     */
    private String furnaceBatch;
    /**
     * 设备编号
     */
    private String deviceId;
    /**
     * 数量
     */
    private BigDecimal amount;
    /**
     * 状态0删除 1正常
     */
    private Integer status;
    /**
     * 创建人id
     */
    private Integer createId;
    /**
     * 创建人
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
     * 修改人
     */
    private String modifier;
    /**
     * 修改时间
     */
    private Date modifyTime;
    /**
     * 产品规格id
     */
    private Integer productSpecId;
    /**
     * 产品规格名称
     */
    private String productSpecName;
    /**
     * 步骤
     */
    private Integer step;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
