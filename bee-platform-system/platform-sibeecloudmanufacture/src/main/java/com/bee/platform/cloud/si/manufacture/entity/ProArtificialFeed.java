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
 * 人工补料表
 * </p>
 *
 * @author huangxin123
 * @since 2019-09-25
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class ProArtificialFeed extends Model<ProArtificialFeed> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Long id;
    /**
     * 炉号id
     */
    private Integer furnaceId;
    /**
     * 炉号名称
     */
    private String furnaceName;
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
     * 产品单位
     */
    private String unit;
    /**
     * 班次:1一班，2二班，3三班
     */
    private Integer shift;
    /**
     * 产品数量
     */
    private BigDecimal num;
    /**
     * 仓库id
     */
    private Integer warehouseId;
    /**
     * 仓库名称
     */
    private String warehouseName;
    /**
     * 添加原因
     */
    private String addReason;
    /**
     * 下料时间
     */
    private Date blankingTime;
    /**
     * 创建人id
     */
    private Integer createId;
    /**
     * 工厂id
     */
    private Integer factoryId;
    /**
     * 企业id
     */
    private Integer companyId;
    /**
     * 修改人id
     */
    private Integer modifyId;
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
    /**
     * 成品id
     */
    private Integer finishProductId;
    /**
     * 成品名称
     */
    private String finishProductName;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
