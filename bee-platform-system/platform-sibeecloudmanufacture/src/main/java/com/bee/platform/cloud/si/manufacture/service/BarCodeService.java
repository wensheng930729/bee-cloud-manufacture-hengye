package com.bee.platform.cloud.si.manufacture.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.entity.BarCode;

import java.util.List;

/**
 * <p>
 * 条形码表 服务类
 * </p>
 *
 * @author MP123
 * @since 2019-10-08
 */
public interface BarCodeService extends IService<BarCode> {

    public BarCode getByCode(String code);

    public boolean updateCodeUsed(BarCode barCode);

    public boolean updateCodeBatchUsed(List<String> codeList);

    public BarCode checkCodeExist(String code);


}
