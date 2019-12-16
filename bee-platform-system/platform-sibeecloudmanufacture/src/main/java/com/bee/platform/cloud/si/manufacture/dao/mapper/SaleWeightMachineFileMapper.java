package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.bee.platform.cloud.si.manufacture.entity.BuyWeightMachineFile;
import com.bee.platform.cloud.si.manufacture.entity.SaleWeightMachineFile;
import com.baomidou.mybatisplus.mapper.BaseMapper;

import java.util.List;

/**
 * @Description  磅单附件信息 Mapper 接口
 * @author chenxm66777123
 * @Date 2019/9/26 16:06
 * @version 1.0.0
 */
public interface SaleWeightMachineFileMapper extends BaseMapper<SaleWeightMachineFile> {

    /**
     * @Description 批量保存附件信息
     * @author chenxm66777123
     * @Date 2019/9/24 14:35
     * @version 1.0.0
     */
    int batchSaveFile(List<SaleWeightMachineFile> saleWeightMachineFileList);

}
