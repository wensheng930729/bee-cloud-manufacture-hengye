package com.bee.platform.cloud.si.manufacture.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * @ClassName: BuyGoodsPendingStorage
 * @Description: 采购待入库货物（以车为单位）
 * @Author: fei.sun
 * @Date: 2019/9/23 13:30
 * @Version: 1.0
 */
@Data
@TableName(value = "buy_product_pending_storage")
@Accessors(chain = true)
public class BuyGoodsPendingStorage extends Model<BuyGoodsPendingStorage> {

    @TableId(type = IdType.AUTO)
    private Long id;
    /**
     * 待入库产品业务Id
     */
    private String buyProductPendingStorageId;

    /**
     * 磅单id
     */
    private String machineId;
    /**
     * 工厂id
     */
    private Integer factoryId;
    /**
     * 当前登录的企业id
     */
    private Integer orgId;
    /**
     * 合同编号
     */
    private String contractId;
    /**
     * 车牌号
     */
    private String licensePlateNumber;
    /**
     * 产品数量
     */
    private BigDecimal productNumber;
    /**
     * 产品入库时的实际数量
     */
    private BigDecimal actualProductNumber;
    /**
     * 产品Id
     */
    private Integer productId;
    /**
     * 产品名称
     */
    private String productName;
    /**
     * 产品单位
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
     * 到厂时间
     */
    private LocalDateTime arrivalTime;
    /**
     * 化验结果
     */
    private Integer analysisResult;
    /**
     * 处理方式
     */
    private Integer processMode;
    /**
     * 是否已入库（0待入库；1已入库）
     */
    private Integer putStorage;
    /**
     * 仓库id
     */
    private Integer storageId;
    /**
     * 仓库名称
     */
    private String storageName;
    /**
     * 入库时间
     */
    private LocalDateTime storageTime;
    /**
     * 磅房备注
     */
    private String remark;
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
