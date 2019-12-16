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
 * 领用出库明细表
 * </p>
 *
 * @author MP123
 * @since 2019-09-27
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class PickOutStorageDetail extends Model<PickOutStorageDetail> {

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
     * 仓库id
     */
    private Integer storageId;
    /**
     * 仓库名称
     */
    private String storageName;
    /**
     * 领用物id
     */
    private Integer productId;
    /**
     * 领用数量
     */
    private BigDecimal productNumber;
    /**
     * 领用人
     */
    private String receiver;
    /**
     * 产品规格id
     */
    private Integer productSpecId;
    /**
     * 产品规格名称
     */
    private String productSpecName;
    /**
     * 领取时间
     */
    private LocalDateTime receiveTime;
    /**
     * 数据状态（0以删除，1使用中）
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
    private LocalDateTime modifyTime;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
