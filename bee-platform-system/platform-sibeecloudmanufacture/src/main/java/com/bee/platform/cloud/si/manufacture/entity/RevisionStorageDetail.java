package com.bee.platform.cloud.si.manufacture.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.enums.IdType;
import java.math.BigDecimal;
import java.time.LocalDateTime;
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
 * 盘库明细表
 * </p>
 *
 * @author MP123
 * @since 2019-09-28
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class RevisionStorageDetail extends Model<RevisionStorageDetail> {

    private static final long serialVersionUID = 1L;

    /**
     * 自增主键
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Long id;
    /**
     * 工厂id
     */
    private Integer factoryId;
    /**
     * 当前登录的企业id
     */
    private Integer orgId;
    /**
     * 货物id
     */
    private Integer productId;
    /**
     * 货物名称
     */
    private String productName;
    /**
     * 调整后数量
     */
    private BigDecimal reviseProductNumber;
    /**
     * 盘库数量
     */
    private BigDecimal reviseAmount;
    /**
     * 货物数量
     */
    private BigDecimal currentProductNumber;
    /**
     * 货物单位
     */
    private String productUnit;
    /**
     * 仓库id
     */
    private Integer storageId;
    /**
     * 仓库名称
     */
    private String storageName;
    /**
     * 盘库原因
     */
    private String reason;
    /**
     * 产品规格id
     */
    private Integer productSpecId;
    /**
     * 产品规格名称
     */
    private String productSpecName;
    /**
     * 数据状态0删除1正常
     */
    private Integer status;
    /**
     * 创建人id
     */
    private Integer createId;
    /**
     * 创建人名称
     */
    private String creator;
    /**
     * 创建/申请时间
     */
    private LocalDateTime createTime;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
