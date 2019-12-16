package com.bee.platform.cloud.si.manufacture.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.annotations.Version;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * @ClassName: StorageDetail
 * @Description: 库存总量
 * @Author: fei.sun
 * @Date: 2019/9/23 11:31
 * @Version: 1.0
 */
@Data
@Accessors(chain = true)
@TableName(value = "storage_inventory")
public class StorageInventory extends Model<StorageInventory> {

    @TableId(type = IdType.AUTO)
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
     * 货物id
     */
    private Integer productId;
    /**
     * 产品规格id
     */
    private Integer productSpecId;
    /**
     * 产品规格名称
     */
    private String productSpecName;
    /**
     * 货物名称
     */
    private String productName;
    /**
     * 货物数量
     */
    private BigDecimal productNumber;
    /**
     * 货物单位
     */
    private String productUnit;
    /**
     * 数据状态（数据状态0删除1正常）
     */
    private Integer status;
    /**
     * 数据版本号
     */
    private Long version;
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

    public StorageInventory() {
    }

    public StorageInventory(Long id, BigDecimal productNumber, Integer modifyId, String modifier, LocalDateTime modifyTime) {
        this.id = id;
        this.productNumber = productNumber;
        this.modifyId = modifyId;
        this.modifier = modifier;
        this.modifyTime = modifyTime;
    }
}
