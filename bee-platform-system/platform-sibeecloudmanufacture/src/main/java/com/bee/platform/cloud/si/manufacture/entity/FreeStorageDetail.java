package com.bee.platform.cloud.si.manufacture.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableName;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * @ClassName: FreeStorageDetail
 * @Description: 自由入库明细表
 * @Author: fei.sun
 * @Date: 2019/9/24 16:35
 * @Version: 1.0
 */
@Data
@Accessors(chain = true)
@TableName(value = "free_storage_detail")
public class FreeStorageDetail extends Model<FreeStorageDetail> {
    /**
     * 自增主键
     */
    private Long id;
    /**
     * 业务唯一Id
     */
    private String freeStorageDetailId;
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
     * 产品id
     */
    private Integer productId;
    /**
     * 产品数量
     */
    private BigDecimal productNumber;
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
     * 仓库编号
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
