import React, { Component } from 'react';
import { message } from 'antd';
import { connect } from 'dva';
import { FormItemRangePicker, FormItemSelect, FormItemInputSearch } from '@/components/FormWidget';
import { SearchFormHOC, TableForParentTab } from '@/components/TableForParentTab';

const params = {
  name: 'huhao',
};

const MyFormSearch = props => {
  const { getFieldDecorator, furnaceOptions, shiftCodeOptions, productsOptions } = props;
  const formItemLayout = {};

  return (
    <>
      {/* <FormItemInputSearch
        getFieldDecorator={getFieldDecorator}
        label="生产编号"
        fieldId="productionNo"
        required={false}
        inputProps={{ placeholder: '' }}
        formItemLayout={formItemLayout}
      /> */}
      <FormItemSelect
        getFieldDecorator={getFieldDecorator}
        label="产品"
        fieldId="productId"
        required={false}
        selectProps={{ options: productsOptions }}
        formItemLayout={formItemLayout}
      />
      <FormItemSelect
        getFieldDecorator={getFieldDecorator}
        label="炉号"
        fieldId="furnaceId"
        required={false}
        selectProps={{ options: furnaceOptions }}
        formItemLayout={formItemLayout}
      />
      <FormItemSelect
        getFieldDecorator={getFieldDecorator}
        label="班次"
        fieldId="shiftCode"
        required={false}
        selectProps={{ options: shiftCodeOptions }}
        formItemLayout={formItemLayout}
      />

      <FormItemRangePicker
        getFieldDecorator={getFieldDecorator}
        label="生产日期"
        fieldId="dateRange"
        required={false}
        datePickerProps={{ format: 'YYYY-MM-DD' }}
        formItemLayout={formItemLayout}
      />
    </>
  );
};

const NewIndex = SearchFormHOC(params)(MyFormSearch);

const NewCom = TableForParentTab(params)(NewIndex);

@connect(({ YieldAnalysisModel, ReportFormCommonModel, loading }) => ({
  furnaceOptions: ReportFormCommonModel.furnaceOptions,
  shiftCodeOptions: ReportFormCommonModel.shiftCodeOptions,
  productsOptions: ReportFormCommonModel.productsOptions,

  listSearchParams: YieldAnalysisModel.listSearchParams,
  parentTabs: YieldAnalysisModel.parentTabs,
  dataList: YieldAnalysisModel.dataList,
  columns: YieldAnalysisModel.columns,

  loading: loading.effects['YieldAnalysisModel/getListEffect'],
}))
class Index extends Component {
  componentDidMount() {
    try {
      _czc1.push(['_trackEvent', '查看报表', '查看产量分析', '', '', '']);
    } catch (error) {}

    const defaultTabPagesParams = {
      type: 'throughput_analysis',
    };

    this.getTabPages(defaultTabPagesParams);
  }

  // 表单查询改变事件
  handleFormChange = field => {
    this.getTableData(field);
  };

  // 搜索按钮 点击事件
  handleSearch = () => {
    this.getTableData();
  };

  // tab切换事件
  handleTabChange = reportType => {
    const data = { reportType };
    this.handleFormChange(data);
  };

  // 表格分页事件
  handleTableChange = payload => {
    this.getTableData(payload);
  };

  // 是否能查看当前页面
  getTabPages = payload => {
    const { dispatch } = this.props;
    dispatch({
      type: 'YieldAnalysisModel/getTabPagesEffect',
      payload,
      callback: (data, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        } else {
          this.getSearchOptions();
        }
      },
    });
  };

  // 获取查询条件中的 下拉框数据，并设置 默认查询条件
  getSearchOptions = payload => {
    const { dispatch } = this.props;
    dispatch({
      type: 'ReportFormCommonModel/fetchFurnacesShiftCodeEffect',
      payload,
      callback: (data, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        } else if (status === 'ok') {
          const { furnaceOptions, shiftCodeOptions, productsOptions } = data;

          const newlistSearchParams = {
            productId: productsOptions[0].value,
            furnaceId: furnaceOptions[0].value,
            shiftCode: shiftCodeOptions[0].value,
          };

          this.handleFormChange(newlistSearchParams);
        }
      },
    });
  };

  // 获取表格数据
  getTableData = payload => {
    // 表格之前的查询条件
    const { dispatch } = this.props;

    dispatch({
      type: 'YieldAnalysisModel/getListEffect',
      payload,
      callback: (data, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        }
      },
    });
  };

  render() {
    const {
      furnaceOptions,
      shiftCodeOptions,
      productsOptions,

      loading,
      listSearchParams,
      columns,
      dataList,
      parentTabs,
    } = this.props;

    const searchFormProps = {
      furnaceOptions,
      shiftCodeOptions,
      productsOptions,

      dataDetail: listSearchParams,
      handleSearch: this.handleSearch,
      onChange: this.handleFormChange,
    };

    const selfProps = {
      loading,
      columns,
      data: dataList,

      handleTabChange: this.handleTabChange,
      handleTableChange: this.handleTableChange,

      parentTabs: null,
      childTabs: null,

      ...searchFormProps,
    };
    if (parentTabs.length === 0) {
      return <h1 style={{ marginLeft: '40%' }}>请联系管理员开启当前页面</h1>;
    }
    return <NewCom {...selfProps} />;
  }
}

export default Index;
