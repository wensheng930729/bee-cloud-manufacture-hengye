import { message, Form } from 'antd';
import React, { useState, useEffect } from 'react';
import { connect } from 'dva';
import moment from 'moment';

import {
  FormItemInputSearch,
  FormItemRangePicker,
  FormItemSelect,
  TableWrapper,
  CustomModalHOC,
  CustomFormHOC,
} from '@/components/FormWidget';

import { selectFinishedStorageByConditional } from '../services/productStorageService';

// 顶部表单查询组件
const MyFormSearch = props => {
  const {
    form: { getFieldDecorator },
    furnaceOptions,
    shiftCodeOptions,
  } = props;
  const formItemLayout = {};

  return (
    <>
      <FormItemInputSearch
        getFieldDecorator={getFieldDecorator}
        label=""
        fieldId="tonBagNumber"
        required={false}
        formItemLayout={formItemLayout}
        inputProps={{ placeholder: '按入吨袋编号查询' }}
      />

      <FormItemSelect
        getFieldDecorator={getFieldDecorator}
        label="炉号"
        fieldId="furnaceNumber"
        required={false}
        selectProps={{ options: furnaceOptions }}
        formItemLayout={formItemLayout}
      />
      <FormItemSelect
        getFieldDecorator={getFieldDecorator}
        label="班次"
        fieldId="scheduling"
        required={false}
        selectProps={{ options: shiftCodeOptions }}
        formItemLayout={formItemLayout}
      />

      <FormItemRangePicker
        getFieldDecorator={getFieldDecorator}
        label="班次日期"
        fieldId="dateRange"
        required={false}
        datePickerProps={{ format: 'YYYY-MM-DD' }}
        formItemLayout={formItemLayout}
      />
    </>
  );
};

//  --------确认入库的modal弹窗
const ComfirmForm = props => {
  const {
    form: { getFieldDecorator },
    storageOptions,
    productNumber,
  } = props;
  const formItemLayout = {
    labelCol: { span: 4 },
    wrapperCol: { span: 16 },
  };

  return (
    <>
      <Form.Item label="入库数量" {...formItemLayout}>
        {productNumber}吨
      </Form.Item>
      <FormItemSelect
        getFieldDecorator={getFieldDecorator}
        label="仓库"
        fieldId="storageId"
        required
        selectProps={{ options: storageOptions }}
        formItemLayout={formItemLayout}
      />
    </>
  );
};
// 创建表单
const NewComfirmForm = CustomFormHOC(ComfirmForm);
// 创建modal
const NewComfirmFormModal = CustomModalHOC(NewComfirmForm);

const Index = props => {
  const { dispatch, furnaceOptions, shiftCodeOptions, storageOptions } = props;
  // 前端默认对 日期列降序排列
  const [sortOrder, setSortOrder] = useState('descend');

  // tab栏目数据
  const parentTabs = [
    { value: '0', label: '待入库' },
    { value: '1', label: '已入库' },
  ];
  // 当前TAB栏目
  const [tabKey, setTabKey] = useState('0');
  // 弹窗是否可见
  const [confirmModalVisible, setConfirmModalVisible] = useState(false);

  // 这里用于请求 顶部搜索表单中的下拉框数据
  useEffect(() => {
    // 这里在mount时执行一次
    dispatch({
      type: 'productStorageModel/fetchInitOptionsEffect',
      payload: {},
      callback: (_, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        }
      },
    });
  }, []);

  const columns = [
    {
      title: '吨袋编号',
      dataIndex: 'tonBagNumber',
    },
    {
      title: '炉号',
      dataIndex: 'furnaceNumber',
    },
    {
      title: '班次',
      dataIndex: 'scheduling',
    },
    {
      title: '出炉批次',
      dataIndex: 'furnaceTimes',
    },
    {
      title: '产品名称',
      dataIndex: 'productName',
    },
    {
      title: '规格',
      dataIndex: 'productSpecName',
    },
    {
      title: tabKey === '0' ? '待入库数量' : '已入库数量',
      dataIndex: 'productNumber',
    },
    {
      title: '班次日期',
      dataIndex: 'shiftTime',
      sortOrder,
      sorter: (a, b) => {
        const yearA = a.shiftTime.slice(0, 4);
        const monthA = a.shiftTime.slice(5, 7);
        const dayA = a.shiftTime.slice(8, 10);

        const newAStr = `${yearA}-${monthA}-${dayA}`;

        const yearB = b.shiftTime.slice(0, 4);
        const monthB = b.shiftTime.slice(5, 7);
        const dayB = b.shiftTime.slice(8, 10);

        const newBStr = `${yearB}-${monthB}-${dayB}`;

        const bool = moment(newAStr).isBefore(newBStr);

        return bool;
      },

      ellipsis: true,
    },
  ];

  if (tabKey === '1') {
    columns.splice(8, 0, {
      title: '入库日期',
      dataIndex: 'storageTime',
      sortOrder,
      sorter: (a, b) => {
        const yearA = a.storageTime.slice(0, 4);
        const monthA = a.storageTime.slice(5, 7);
        const dayA = a.storageTime.slice(8, 10);

        const newAStr = `${yearA}-${monthA}-${dayA}`;

        const yearB = b.storageTime.slice(0, 4);
        const monthB = b.storageTime.slice(5, 7);
        const dayB = b.storageTime.slice(8, 10);

        const newBStr = `${yearB}-${monthB}-${dayB}`;

        const bool = moment(newAStr).isBefore(newBStr);

        return bool;
      },

      ellipsis: true,
    });
  }

  // 表格改变事件， 手动控制排序规则
  const handleTableChange = (pagination, filter, sorter) => {
    if (sortOrder !== sorter.order) {
      setSortOrder(sorter.order);
    }
  };

  const commonTableProps = {
    parentTabs,
    // 顶部搜索表单中的属性，
    searchFormProps: {
      furnaceOptions,
      shiftCodeOptions,
    },
    // 设置表单默认值， 默认查询条件
    searchParams: {
      currentPage: 1,
      orderStage: '',
      pageSize: 10,
      searchCount: true,
      // 炉号
      furnaceNumber: '0',
      // 班次
      scheduling: '0',
      // 是否已入库
      putStorage: tabKey,
      // 吨袋编号
      tonBagNumber: '',

      dateRange: {
        startTime: moment()
          .subtract(30, 'days')
          .format('YYYY-MM-DD'),
        endTime: moment().format('YYYY-MM-DD'),
      },
    },
    // 表格改变事件
    handleTableChange,
    // tab栏目改变事件
    onTabChange: activeKey => setTabKey(activeKey),
  };

  const rowKey = 'finishedProductPendingStorageId';

  // 当tabkey===1 时的  TableWrapper 组件props
  const CusTomTableProps = {
    ...commonTableProps,
    // 表格属性
    tableProps: {
      columns,
      rowKey,
    },
  };
  // 当tabkey===0 时的  TableWrapper 组件props
  const [selectedWeight, setSelectedWeight] = useState(0);
  const [selectedRowsData, setSelectedRowsData] = useState([]);

  const CusTomTablePropsForWait = {
    ...commonTableProps,
    tableProps: {
      hasRowSelection: true,
      columns,
      rowKey,
    },
    handleSelectRows: (selectedRowKeys, selectedRows) => {
      if (selectedRows.length > 0) {
        const datas = selectedRows.map(item => item.productNumber);
        const weight = datas.reduce((prev, curr) => Number(prev) + Number(curr)).toFixed(3);
        setSelectedWeight(weight);
      }
      setSelectedRowsData(selectedRows);
    },

    operateText: '确认入库',
    onOperateClick: () => {
      if (selectedRowsData.length > 0) {
        setConfirmModalVisible(true);
      } else {
        message.warning('请选择数据');
      }
    },
  };

  // modal弹窗props
  const ComfirmFormModalProps = {
    visible: confirmModalVisible,
    onOk: fields => {
      try {
        _czc1.push(['_trackEvent', '成品入库', '确认入库', '', '', '']);
      } catch (error) {}
      const finishedProductStorageDTOS = selectedRowsData.map(item => ({
        finishedProductPendingStorageId: item.finishedProductPendingStorageId,
        productId: item.productId,
        productName: item.productName,
        productNumber: item.productNumber,
        productUnit: item.productUnit,
        tonBagNumber: item.tonBagNumber,
      }));
      const payload = {
        finishedProductStorageDTOS,
        storageId: fields.storageId,
        storageName: storageOptions.filter(item => item.value === fields.storageId)[0].label,
      };
      dispatch({
        type: 'productStorageModel/saveProductEffect',
        payload,
        callback: (_, status, msg) => {
          if (status === 'error') {
            message.error(msg);
          } else {
            message.success(msg);
            setConfirmModalVisible(false);
          }
        },
      });
    },
    onCancel: () => setConfirmModalVisible(false),
    title: '确认入库',
    ComProps: {
      storageOptions,
      productNumber: selectedWeight,
    },
  };

  const myProps = tabKey === '0' ? { ...CusTomTablePropsForWait } : { ...CusTomTableProps };

  const paramsHandler = params => {
    const { dateRange: newdateRange, scheduling, furnaceNumber, ...other } = params;
    const furnaceNumberData = furnaceOptions.filter(item => item.value === furnaceNumber);
    const schedulingData = shiftCodeOptions.filter(item => item.value === scheduling);
    const requestParams = {
      startTime: newdateRange.startTime,
      endTime: newdateRange.endTime,
      putStorage: tabKey,
      furnaceNumber:
        furnaceNumberData.length > 0 && furnaceNumberData[0].label !== '全部'
          ? furnaceNumberData[0].label
          : '',
      scheduling:
        schedulingData.length > 0 && schedulingData[0].label !== '全部'
          ? schedulingData[0].label
          : '',
      ...other,
    };
    return requestParams;
  };

  return (
    <>
      <TableWrapper
        {...myProps}
        listService={selectFinishedStorageByConditional}
        currentTabKey={tabKey}
        paramsHandler={paramsHandler}
      >
        <MyFormSearch />
      </TableWrapper>
      <NewComfirmFormModal {...ComfirmFormModalProps} />
    </>
  );
};

// 绑定redux
export default connect(state => {
  const { furnaceOptions, shiftCodeOptions, storageOptions } = state.productStorageModel;

  return {
    furnaceOptions,
    shiftCodeOptions,
    storageOptions,
  };
})(Index);
