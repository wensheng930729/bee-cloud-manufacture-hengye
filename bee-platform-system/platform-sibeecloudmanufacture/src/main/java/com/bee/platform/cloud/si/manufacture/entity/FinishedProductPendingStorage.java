package com.bee.platform.cloud.si.manufacture.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableName;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * @ClassName: FinishedProductPendingStorage
 * @Description: 产成品待入库明细表
 * @Author: fei.sun
 * @Date: 2019/9/25 10:23
 * @Version: 1.0
 */

@Data
@Accessors(chain = true)
@TableName("finished_product_pending_storage")
public class FinishedProductPendingStorage extends Model<FinishedProductPendingStorage> {
    /**
     * 自增主键
     */
    private Long id;
    /**
     * 业务唯一标识id
     */
    private String finishedProductPendingStorageId;
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
     * 吨袋编号
     */
    private String tonBagNumber;
    /**
     * 吨袋重量
     */
    private BigDecimal productNumber;
    /**
     * 货物id
     */
    private Integer productId;
    /**
     * 样品名称
     */
    private String productName;
    /**
     * 货物数量单位
     */
    private String productUnit;
    /**
     * 产品规格id
     */
    private Integer productSpecId;
    /**
     * 产品规格名称
     */
    private String productSpecName;
    /**
     * 炉号
     */
    private String furnaceNumber;
    /**
     * 炉次
     */
    private String furnaceTimes;
    /**
     * 班次
     */
    private String scheduling;
    /**
     * 入库时间
     */
    private LocalDateTime storageTime;
    /**
     * 是否已入库（0待入库；1已入库）
     */
    private Integer putStorage;
    /**
     * 数据状态（数据状态0删除1正常）
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
    private LocalDateTime createTime;
    /**
     * 修改人
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
