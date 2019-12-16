package com.bee.platform.cloud.si.manufacture.entity;

import java.io.Serializable;

import java.math.BigDecimal;
import java.util.Date;
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
 * 下料明细表
 * </p>
 *
 * @author huangxin123
 * @since 2019-10-21
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class ProBlankingDetail extends Model<ProBlankingDetail> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Long id;
    /**
     * 下料主表id
     */
    private Long blankingId;
    /**
     * 产品id
     */
    private Integer productId;
    /**
     * 产品名称
     */
    private String productName;
    /**
     * 产品规格id
     */
    private Integer productSpecId;
    /**
     * 产品规格名称
     */
    private String productSpecName;
    /**
     * 产品数量
     */
    private BigDecimal num;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 更新时间
     */
    private Date modifyTime;
    /**
     * 数据状态:0删除1正常
     */
    private Integer status;
    /**
     * 料批类型:0主料1辅料
     */
    private Integer type;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
